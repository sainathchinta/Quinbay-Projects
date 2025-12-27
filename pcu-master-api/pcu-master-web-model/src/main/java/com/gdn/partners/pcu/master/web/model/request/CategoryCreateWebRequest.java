package com.gdn.partners.pcu.master.web.model.request;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * Created by govind on 15/11/2018 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CategoryCreateWebRequest{
  private String catalogId;
  private CategoryInfoUpdateWebRequest categoryInfoDetail;
  private CategoryMappingsUpdateWebRequest categoryMappingsDetail;
}
