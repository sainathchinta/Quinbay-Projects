package com.gdn.x.productcategorybase.entity.MailEvent;

public enum ConfigMailEventEnums {
  MERCHANT_CONFIG_ADDED("merchant-mail-configuration-added"),
  CATEGORY_CONFIG_ADDED("category-mail-configuration-added");

  private final String configurationType;

  ConfigMailEventEnums(String configurationType){
    this.configurationType = configurationType;
  }

  public String getConfigurationType(){
    return configurationType;
  }
}
