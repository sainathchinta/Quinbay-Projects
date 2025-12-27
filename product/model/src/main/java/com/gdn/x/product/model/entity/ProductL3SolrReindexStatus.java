package com.gdn.x.product.model.entity;

import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductReindexStatus;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = ProductL3SolrReindexStatus.DOCUMENT_NAME)
public class ProductL3SolrReindexStatus extends GdnBaseMongoEntity {

  public static final String DOCUMENT_NAME = "prd_product_l3_solr_reindex_status";

  @Indexed(unique = true)
  @Field(value = ProductFieldNames.PRODUCT_SKU)
  private String productSku;

  @Indexed
  @Enumerated(EnumType.STRING)
  @Field(value = ProductFieldNames.PRODUCT_REINDEX_STATUS)
  private ProductReindexStatus productReindexStatus;
}
