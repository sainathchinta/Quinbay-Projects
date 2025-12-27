package com.gdn.x.product.model.entity;

import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductReindexStatus;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;


@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Document(collection = DeferredSolrReindexItem.DOCUMENT_NAME)
public class DeferredSolrReindexItem extends GdnBaseMongoEntity {

  public static final String DOCUMENT_NAME = "deferred_solr_reindex_item";

  @Field(value = ProductFieldNames.ITEM_SKU)
  private String itemSku;

  @Field(value = ProductFieldNames.REINDEX_TYPE)
  private String reindexType;

  @Indexed
  @Enumerated(EnumType.STRING)
  @Field(value = ProductFieldNames.PRODUCT_REINDEX_STATUS)
  private ProductReindexStatus productReindexStatus;
}
