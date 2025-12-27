package com.gdn.partners.pbp.entity.workflow.product;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;

@Entity
@Table(name = ProductWfHistory.TABLE_NAME)
public class ProductWfHistory extends GdnBaseEntity {

  private static final long serialVersionUID = -6324994383288864832L;
  public static final String TABLE_NAME = "PBP_PRODUCT_WORKFLOW_HISTORY";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_STATE = "STATE";

  @Column(name = ProductWfHistory.COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = ProductWfHistory.COLUMN_STATE, nullable = false)
  private String state;

  public ProductWfHistory() {}

  public ProductWfHistory(String productCode, String state) {
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
    return String.format("ProductWfHistory [productCode=%s, state=%s]", productCode, state);
  }

}
