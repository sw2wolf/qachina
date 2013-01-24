$mkdir -p $HOME/.chicken

%chicken-install -i $HOME/.chicken
%setenv CHICKEN_REPOSITORY $HOME/.chicken
%setenv CHICKEN_INSTALL_PREFIX $HOME/.chicken
%setenv CHICKEN_INCLUDE_PATH $HOME/.chicken/share/chicken

That's all! Now you can easily install eggs by typing:
%chicken-install egg

Please note:
Platform-dependent binaries (shared libraries or *.so) are stored in CHICKEN_REPOSITORY.
Platform-independent sources (*.scm) are stored in CHICKEN_INSTALL_PREFIX/share/chicken.

Now you can load an egg:
(require-extension binary)

; or

(include "source")
