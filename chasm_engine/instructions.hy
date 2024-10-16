"
This module provides template functions and macros for handling and applying templates.

The module includes the following main functions and macros:

1. find_template: locate template files in the 'templates' directory.
2. complete_template: load and apply templates with given parameters.
3. deftemplate_group: define functions for applying template groups.
4. deftemplate: define functions for applying specific templates.

"

(require hyrule.argmove [-> ->>])

(import hyjinx.lib [first slurp filenames])

(import pathlib [Path])

(import tomllib)

(import chasm_engine.lib [ChasmEngineError config])


(defn find-templates []
  "All template toml files. Just returns the template name (not the path)"
  (lfor template-file
    (filenames (. (Path __file__) parent) "templates")
    :if (.endswith template-file ".toml")
    (. (Path template-file) stem)))

(defn find-template [#^ str name]
  "Returns the Path of the template name."
  (let [fname (+ name ".toml")]
    (Path (. (Path __file__) parent) "templates" fname)))

(defn complete-template [#^ str template-file #^ str template-name #** kwargs]
  "Locate a template toml file under `$module_dir/templates`.
  Load the template with name `template-file.toml` from the templates directory
  and apply python string `.format` method.
  Replace each `{kwarg}` with its value to form the one-shot user prompt."
  (let [template (config template-name :file (find-template template-file))]
    (if template
      (.format template #** kwargs)
      (raise (ChasmEngineError f"Template '{template-name}' not found in file '{template-file}.toml'.")))))

(defmacro deftemplate [template-file]
  "Macro that defines a function named after `template-file`.
  For example, `(deftemplate \"context\")` creates a function
  `context` that when called like `(context \"world\" #** kwargs)`,
  applies that template."
  (let [docstr (.format "Applies the kwargs to one of the `{templates}`\ntemplates defined in `{template_file}.toml`, specified by the `template-name` arg."
                        :template-file template-file
                        :templates (.join "`, `" (.keys (tomllib.loads (slurp (find-template template-file))))))
        name (if (isinstance template-file hy.models.Symbol)
               (str template-file)
               template-file.__name__)]
    `(defn ~template-file [template-name #** kwargs]
       ~docstr
       (import chasm-engine.instructions [find-template complete-template])
       (complete-template ~name template-name #** kwargs))))

(defmacro def-fill-template [template-file instruction [system-prompt None]] 
  "Macro that defines a function named as `template-file`-`template-name`.
  For example, `(def-fill-template \"context\" \"world\")` creates a function
  `context-world` that when called like `(context-world messages #** kwargs)`,
  applies that template to give a user instruction following the provided messages.
  Optionally uses `system_prompt` template from that same file."
  (let [template (str template-file)
        instruction (str instruction)
        system-str (if system-prompt
                     (str system-prompt)
                     "")
        docstr (.format "Applies the kwargs to template `{instruction}`\n as defined in `{template_file}.toml`.\n{system_doc}"
                        :instruction (str instruction)
                        :template-file (str template-file)
                        :system-doc (if system-prompt
                                      f"\nAlso uses {system-str} as the system prompt."
                                      ""))
        fn-name (hy.models.Symbol (.join "-" [template instruction]))]
    (if system-prompt
      `(defn :async ~fn-name [[messages []] [provider "backend"] #** kwargs]
         ~docstr
         (import chasm_engine.chat [respond])
         (import chasm-engine.instructions [find-template complete-template])
         (let [user-msg (complete-template ~template ~instruction #** kwargs)
               system-msg (complete-template ~template (str ~system-str) #** kwargs)]
           (await (respond [(system system-msg)
                            #* messages
                            (user user-msg)]
                           :provider provider))))
      `(defn :async ~fn-name [[messages []] [provider "backend"] #** kwargs]
         ~docstr
         (import chasm_engine.chat [respond])
         (import chasm-engine.instructions [find-template complete-template])
         (let [user-msg (complete-template ~template ~instruction #** kwargs)]
           (await (respond [#* messages
                            (user user-msg)]
                           :provider provider)))))))

