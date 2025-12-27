package com.gdn.partners.product.analytics.model.enums;

public enum FileExtensions {

  GZIP(".gz"),
  NDJSON(".ndjson"),
  CSV(".csv");

  private String extension;

  FileExtensions(String extension){
    this.extension = extension;
  }

  public String getExtension(){
    return extension;
  }

}
