package com.gdn.x.product.model.entity;

import java.util.HashSet;
import java.util.Set;

import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.BusinessPartnerFieldNames;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = false)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = BusinessPartnerPromo.DOCUMENT_NAME)
public class BusinessPartnerPromo extends GdnBaseMongoEntity{

  public static final String DOCUMENT_NAME = "business_partner_promo";

  @Indexed(unique = true)
  @Field(value = BusinessPartnerFieldNames.BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Field(value = BusinessPartnerFieldNames.SELLER_ACTIVE_PROMO_BUNDLING)
  private Set<String> activePromoBundlings = new HashSet<>();

}
