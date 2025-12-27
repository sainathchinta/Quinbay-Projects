package com.gdn.mta.bulk.models.download.responsedata;


import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductImageAndVideoResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -6421723734359896218L;

  private List<ImageBasicInfoResponse> commonImageList = new ArrayList<>();
  private String video;
  private String brand;
  private String productName;
  private String url;
}
