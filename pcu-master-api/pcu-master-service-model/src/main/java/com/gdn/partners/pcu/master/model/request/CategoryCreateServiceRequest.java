package com.gdn.partners.pcu.master.model.request;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 28/11/2018 AD.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CategoryCreateServiceRequest {

  private String storeId;
  private String catalogId;
  private CategoryInfoUpdateServiceRequest categoryInfoUpdateServiceRequest;
  private CategoryMappingsUpdateServiceRequest categoryMappingsUpdateServiceRequest;
  private Date createdDate;
  private String createdBy;
}
