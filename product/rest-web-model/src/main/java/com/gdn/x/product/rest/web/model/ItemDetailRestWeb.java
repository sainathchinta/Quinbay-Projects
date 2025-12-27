package com.gdn.x.product.rest.web.model;



import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemDetailRestWeb extends BaseResponse {

  public static final class ItemDetailRestWebBuilder implements GdnBaseBuilder<ItemDetailRestWeb> {

    private String itemSku;
    private String itemName;
    private String merchantCode;

    public ItemDetailRestWebBuilder() {}

    @Override
    public ItemDetailRestWeb build() {
      return new ItemDetailRestWeb(this);
    }

    public ItemDetailRestWebBuilder setItemName(String itemName) {
      this.itemName = itemName;
      return this;
    }

    public ItemDetailRestWebBuilder setItemSku(String itemSku) {
      this.itemSku = itemSku;
      return this;
    }

    public ItemDetailRestWebBuilder setMerchantCode(String merchantCode) {
      this.merchantCode = merchantCode;
      return this;
    }

    @Override
    public String toString() {
      return String.format(
          "ItemDetailRestWebBuilder [itemSku=%s, itemName=%s, merchantCode=%s, toString()=%s]",
          this.itemSku, this.itemName, this.merchantCode, super.toString());
    }
  }


  private static final long serialVersionUID = 1L;

  private String itemSku;
  private String itemName;
  private String merchantCode;

  public ItemDetailRestWeb() {}

  public ItemDetailRestWeb(ItemDetailRestWebBuilder builder) {
    this.itemSku = builder.itemSku;
    this.itemName = builder.itemName;
    this.merchantCode = builder.merchantCode;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemName() {
    return this.itemName;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  @Override
  public String toString() {
    return String.format(
        "ItemDetailRestWeb [itemSku=%s, itemName=%s, merchantCode=%s, toString()=%s]",
        this.itemSku, this.itemName, this.merchantCode, super.toString());
  }

}
