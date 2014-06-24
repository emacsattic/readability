# Readability for Emacs

Get Readability's reading list, and read each article on Emacs.

![readability.el](https://raw2.github.com/ShingoFukuyama/images/master/readability-el.gif)

## Requre
+ Emacs 24.3
+ [Readability Account](https://readability.com/)
+ [FontAwesome](http://fortawesome.github.io/Font-Awesome/) (font for icon)
+ [oauth.el](https://github.com/psanford/emacs-oauth)
+ [ov.el](https://github.com/ShingoFukuyama/ov.el)

## Get Started
1. `M-x readability-get-reading-list`.  
Your default browser will present Readability's login page (if you have not been logged in yet).
2. After logged in, authorize this app by clicking "Allow" button.
3. Copy request token on the browser.
4. Paste request token to Emacs mini buffer.
5. Emacs will start fetching reading list.
6. Press "RET" key on any title to show its contents.

Once authorization success, you don't need to login from then on.

If you would like to logout, just do `M-x readability-delete-token-and-file`.


## Customize

### Token file path
```cl
(setq readability-file-location "your/path/to/token_file")
```

### Reading list parameter
```cl
(setq readability-parameters
      '(("archive"  . nil) ;; "0", "1"
        ("favorite" . nil) ;; "0", "1"
        ("order"    . nil) ;; "-date_added", "date_added", "-date_updated", "date_updated"
        ("domain"   . nil) ;; string
        ("tags"     . nil) ;; string
        ))
```

You can specify more parameters: https://www.readability.com/developers/api/reader#idm301959944144


