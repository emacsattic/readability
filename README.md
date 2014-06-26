# Readability for Emacs

Get Readability's reading list, and read each article on Emacs.

![readability.el](https://raw2.github.com/ShingoFukuyama/images/master/readability-el.gif)

## Require
+ Emacs 24.3
+ [Readability Account](https://readability.com/)
+ [FontAwesome](http://fortawesome.github.io/Font-Awesome/) (font for icon)
+ [oauth.el](https://github.com/psanford/emacs-oauth)
+ [async.el](https://github.com/jwiegley/emacs-async)
+ [ov.el](https://github.com/ShingoFukuyama/ov.el)

## Get Started

```cl
(add-to-list 'load-path "/your/path/to/emacs-readability")
(require 'readability)
```

1. `M-x readability-get-reading-list`.  
Your default browser will present Readability's login page (if you have not been logged in yet).
2. After logged in, authorize this app by clicking "Allow" button.
3. Copy request token on the browser.
4. Paste request token to Emacs mini buffer.
5. Emacs will start fetching a reading list.
6. Press "RET" key on any title to show its contents.

Once authorization successed, you don't need to login from then on.

If you would like to logout, just do `M-x readability-delete-token-and-file`.

## Keyboard shortcut

* `RET` on an icon: toggle boolean status of favorite or archive.
* `RET` on a title: open article in the current window.
* `o, O` on a title: open article in another window.
* `C-o` on a title: open article in another window without move the current window.
* `+` on article buffer: font size + 0.1.
* `-` on article buffer: font size - 0.1.
* `F` on article buffer: toggle fonts.

Following keybinds work with latest ov.el(ver 1.0.3)

* `f,b,n,p`: move cursor.
* `h,l,j,k`: move cursor like vi.

## Customize

### Token file path
```cl
(setq readability-file-location "your/path/to/token_file")
```

### Adjust line width and height
```
;; Space between line and line in article buffers, without affect other buffers
(setq readability-line-height-for-article 1.15)
;; Each line's length calculation
(setq readability--line-width-for-article (lambda () (- (window-width) 10)))
```

### Reading list parameter
```cl
(setq readability-parameters
      '(("archive"  . nil)  ;; "0", "1"
        ("favorite" . nil)  ;; "0", "1"
        ("order"    . nil)  ;; "-date_added", "date_added", "-date_updated", "date_updated"
        ("page"     . nil)  ;; "1" ~
        ("per_page" . "50") ;; "1" ~ "50"
        ("domain"   . nil)  ;; string
        ("tags"     . nil)  ;; string
        ))
```

You can specify more parameters: https://www.readability.com/developers/api/reader#idm301959944144

### Toggle fonts
```cl
(setq readability-font-list
      '("Default" ;; "Default" will be replaced with your default font
        "Georgia"
        "Arial"
        "Verdana"))
```

First font is applied when open an article buffer.

Note: fonts for multi-byte are not worked properly.

## Change Log

#### ver 1.1.0
Add asynchronous features to fetching article and toggle status of favorite/archive.



