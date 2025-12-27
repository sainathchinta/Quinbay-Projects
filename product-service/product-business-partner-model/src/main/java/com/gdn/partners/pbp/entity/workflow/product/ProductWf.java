package com.gdn.partners.pbp.entity.workflow.product;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;

@Entity
@Table(name = ProductWf.TABLE_NAME)
public class ProductWf extends GdnBaseEntity {

  private static final long serialVersionUID = -956878702174631838L;
  public static final String TABLE_NAME = "PBP_PRODUCT_WORKFLOW";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_STATE = "STATE";

  @Column(name = ProductWf.COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = ProductWf.COLUMN_STATE, nullable = false)
  private String state;

  public ProductWf() {}

  public ProductWf(String productCode, String state) {
    super();
    this.productCode = productCode;
    this.state = state;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getState() {
    return state;
  }

  public void setState(String state) {
    this.state = state;
  }

  @Override
  public String toString() {
    return String.format("ProductWf [productCode=%s, state=%s]", productCode, state);
  }

}
