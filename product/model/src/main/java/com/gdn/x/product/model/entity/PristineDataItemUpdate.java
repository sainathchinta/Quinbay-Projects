package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
@Document(collection = PristineDataItemUpdate.DOCUMENT_NAME)
public class PristineDataItemUpdate extends GdnBaseMongoEntity {
  private static final long serialVersionUID = -8677501080899382539L;
  public static final String DOCUMENT_NAME = "prd_pristine_update";

  @Indexed(unique = true)
  @Field(value = ProductFieldNames.PRISTINE_ID)
  private String pristineId;

  @Field(value = ProductFieldNames.IS_UPDATED)
  private boolean isUpdated;

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public boolean isUpdated() {
    return isUpdated;
  }

  public void setUpdated(boolean updated) {
    isUpdated = updated;
  }
}
