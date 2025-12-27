package com.gdn.mta.bulk.dto;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TaggedProductFilterResponse extends BaseResponse {

  private static final long serialVersionUID = -6945098892993142844L;

  private String itemSku;
  private String pickupPointCode;
  private List<String> parentCategoryNames;
  private String businessPartnerName;
  private String productTypeName;
  private Date userUpdatedDate;
}