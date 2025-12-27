package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by Vishal on 15/05/18.
 */
public class ProductWipDeleteResponse extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -8322390431846776103L;

  private String productCode;
  private String updatedBy;
  private String notes;

  public ProductWipDeleteResponse() {
  }

  public ProductWipDeleteResponse(String productCode, String updatedBy, String notes) {
    this.productCode = productCode;
    this.updatedBy = updatedBy;
    this.notes = notes;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getUpdatedBy() {
    return updatedBy;
  }

  public void setUpdatedBy(String updatedBy) {
    this.updatedBy = updatedBy;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCode", productCode)
        .append("updatedBy", updatedBy).append("notes", notes).toString();
  }
}
