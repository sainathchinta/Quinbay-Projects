package com.gdn.x.productcategorybase.dto;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 27/11/2018 AD.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CategoryDetailDTO {

  private String id;
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private String catalogId;
  private CategoryMappingsUpdateDTO categoryMappingsDetail;
  private CategoryInfoUpdateDTO categoryInfoDetail;
}
