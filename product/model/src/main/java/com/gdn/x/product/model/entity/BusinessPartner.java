package com.gdn.x.product.model.entity;


import java.util.ArrayList;
import java.util.List;

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
@Document(collection = BusinessPartner.DOCUMENT_NAME)
public class BusinessPartner extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 1L;

  public static final String DOCUMENT_NAME = "business_partner";

  @Indexed(unique = true)
  @Field(value = BusinessPartnerFieldNames.BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Field(value = BusinessPartnerFieldNames.BUSINESS_PARTNER_NAME)
  private String businessPartnerName;

  @Field(value = BusinessPartnerFieldNames.NAME)
  private String name;

  @Field(value = BusinessPartnerFieldNames.BUSINESS_PARTNER_TYPE)
  private String businessPartnerType;

  @Field(value = BusinessPartnerFieldNames.MERCHANT_STATUS)
  private String merchantStatus;

  @Field(value = BusinessPartnerFieldNames.MERCHANT_TYPE)
  private String merchantType;

  @Field(value = BusinessPartnerFieldNames.LINKED_PARTNER_STORE)
  private String linkedPartnerStore;

  @Field(value = BusinessPartnerFieldNames.INTERNATIONAL_FLAG)
  private boolean internationalFlag;

  @Field(value = BusinessPartnerFieldNames.UMKM_FLAG)
  private boolean umkmFlag;

  @Field(value = BusinessPartnerFieldNames.OFFLINE_TO_ONLINE_FLAG)
  private boolean offlineToOnlineFlag;

  @Field(value = BusinessPartnerFieldNames.INVENTORY_FULFILLMENT)
  private String inventoryFulfillment;

  @Field(value = BusinessPartnerFieldNames.ALL_CATEGORY)
  private boolean allCategory;

  @Field(value = BusinessPartnerFieldNames.CNC_ACTIVATED)
  private boolean cncActivated;

  @Field(value = BusinessPartnerFieldNames.SUPPLIER_FLAG)
  private boolean supplierFlag;

  @Field(value = BusinessPartnerFieldNames.CUSTOMER_FLAG)
  private boolean customerFlag;

  @Field(value = BusinessPartnerFieldNames.MERCHANT_FLAG)
  private boolean merchantFlag;

  @Field(value = BusinessPartnerFieldNames.MERCHANT_DELIVERY_TYPE)
  private String merchantDeliveryType;

  @Field(value = BusinessPartnerFieldNames.BUSINESS_PARTNER_ALIAS)
  private String businessPartnerAlias;

  @Field(value = BusinessPartnerFieldNames.SALES_CHANNEL)
  private List<String> salesChannel = new ArrayList<>();

  @Field(value = BusinessPartnerFieldNames.BLIBLI_OMG)
  private Boolean sellerOmg;
}
