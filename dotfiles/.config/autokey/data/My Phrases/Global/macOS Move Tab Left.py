if window.get_active_class() == 'jetbrains-goland.jetbrains-goland':
  keyboard.send_keys("<ctrl>+<alt>+<right>")
else:
  keyboard.send_keys("<ctrl>+<shift>+<page_up>")