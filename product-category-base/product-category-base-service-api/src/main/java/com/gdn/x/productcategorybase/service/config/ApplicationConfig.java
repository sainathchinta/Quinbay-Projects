package com.gdn.x.productcategorybase.service.config;

public class ApplicationConfig {
  private boolean regenerateWhenDeleteAllowedAttribute;

  public ApplicationConfig() {}

  public boolean isRegenerateWhenDeleteAllowedAttribute() {
    return this.regenerateWhenDeleteAllowedAttribute;
  }

  public void setRegenerateWhenDeleteAllowedAttribute(boolean regenerateWhenDeleteAllowedAttribute) {
    this.regenerateWhenDeleteAllowedAttribute = regenerateWhenDeleteAllowedAttribute;
  }

}
