package com.gdn.partners.pcu.external.web.model.request;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 13/12/2018 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductCategoryWebRequest {

  private String id;
  private CatalogWebRequest catalog;
  private String name;
  private String categoryCode;
  private Integer sequence;
  private String shortDescription;
  private byte[] description;
  private byte[] defaultDescription;
  private String state;
  private boolean display;
  private Integer logisticAdjustment;
  private boolean warranty;
  private boolean needIdentity;
  private boolean activated;
  private boolean viewable;
  private Integer internalActivationInterval;
}
