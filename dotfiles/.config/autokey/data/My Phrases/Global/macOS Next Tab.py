if window.get_active_class() == 'jetbrains-goland.jetbrains-goland':
  keyboard.send_keys("<ctrl>+<shift>+]")
else:
  keyboard.send_keys("<ctrl>+<page_down>")