(declare-project
  :name "thehouse"
  :description ```Horror game ```
  :version "1.0.0"
  :dependencies [{:url "https://github.com/ianthehenry/judge.git"
                  :tag "v2.7.0"}
                 {:url "file:///home/grfork/reps/jaylib"
                  :branch "meine"}])

(declare-executable
  :name "thehouse"
  :entry "thehouse/init.janet")

(task "test" [] (shell "jpm_tree/bin/judge"))
