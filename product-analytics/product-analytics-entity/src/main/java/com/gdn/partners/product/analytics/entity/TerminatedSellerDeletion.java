package com.gdn.partners.product.analytics.entity;

import com.gdn.partners.product.analytics.model.TerminatedSellerDeletionFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document(collection = TerminatedSellerDeletionFields.COLLECTION_NAME)
public class TerminatedSellerDeletion extends GdnBaseMongoEntity{

  private static final long serialVersionUID = -5505098340127911873L;

  @Field(TerminatedSellerDeletionFields.PRODUCT_CODE)
  private String productCode;

  @Field(TerminatedSellerDeletionFields.SELLER_CODE)
  private String sellerCode;

  @Field(TerminatedSellerDeletionFields.PBP)
  private String pbp;

  @Field(TerminatedSellerDeletionFields.PCB)
  private String pcb;

  @Field(TerminatedSellerDeletionFields.PDT)
  private String pdt;

  @Field(TerminatedSellerDeletionFields.X_PRODUCT)
  private String xProduct;

  @Field(TerminatedSellerDeletionFields.FINAL_RESULT)
  private String finalResult;

  @Field(TerminatedSellerDeletionFields.NOTES)
  private String notes;

  @Field(TerminatedSellerDeletionFields.SKIPPED)
  private boolean skipped;

  @Field(TerminatedSellerDeletionFields.RETRY_COUNT)
  private int retryCount;

  @Field(TerminatedSellerDeletionFields.PRODUCT_SKU)
  private String productSku;

  @Field(TerminatedSellerDeletionFields.SHARED_PRODUCT)
  private Boolean sharedProduct;
}
