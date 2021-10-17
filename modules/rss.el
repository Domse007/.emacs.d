(use-package elfeed
  :custom
  (elfeed-feeds '(("https://www.reddit.com/r/emacs/top.rss" emacs reddit)
		  ("https://www.reddit.com/r/rust/top.rss" rust reddit)
		  ("https://rss.nytimes.com/services/xml/rss/nyt/Europe.xml" news world))))

(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup))

(provide 'dk/rss)
