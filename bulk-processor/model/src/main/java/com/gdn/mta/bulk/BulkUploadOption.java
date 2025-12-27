package com.gdn.mta.bulk;

public enum BulkUploadOption {

  SHIPPING_TYPE_ASSIGNED_BY_BLIBLI(1, "Melalui partner logistik Blibli"),
  SHIPPING_TYPE_SPECIAL_SHIPPING(2, "Dikirimkan oleh seller"),
  SHIPPING_TYPE_BOPIS(3, "Bopis"),
  SHIPPING_TYPE_ASSIGNED_BY_BLIBLI_EN(1, "Through Blibli logistic partner"),
  SHIPPING_TYPE_SPECIAL_SHIPPING_EN(2, "Shipped by seller"),

  //TODO : need to remove 'SHIPPING_TYPE_MERCHANT_PARTNER' option in next release.
  SHIPPING_TYPE_MERCHANT_PARTNER(2, "Shipment by Merchant Partner"),

  NEED_INSTALLATION_FALSE(false, "Tidak perlu instalasi"),
  NEED_INSTALLATION_TRUE(true, "Perlu instalasi"),
  
  DISPLAY_FALSE(false, "SKU tidak ditampilkan di website"),
  DISPLAY_TRUE(true, "SKU ditampilkan di website"),

  DISPLAY_EN_FALSE(false, "SKUs are not displayed on the website"),
  DISPLAY_EN_TRUE(true, "SKU is displayed on the website"),
  
  BUYABLE_FALSE(false, "SKU tidak bisa dibeli di website"),
  BUYABLE_TRUE(true, "SKU bisa dibeli di website"),

  BUYABLE_EN_FALSE(false, "SKUs cannot be purchased on the website"),
  BUYABLE_EN_TRUE(true, "SKUs can be purchased on the website");
  
  private Object value;
  private String description;
  
  public static BulkUploadOption valueFrom(String description){
    for(BulkUploadOption data : values()){
      if(data.getdescription().equals(description)){
        return data;
      }
    } 
    throw new IllegalArgumentException("No matching constant for description: " + description);
  }
  
  private BulkUploadOption(Object value, String description) {
    this.value = value;
    this.description = description;
  }

  public Object getValue() {
    return value;
  }
  public void setValue(Object value) {
    this.value = value;
  }
  public String getdescription() {
    return description;
  }
  public void setdescription(String description) {
    this.description = description;
  }
  
}
