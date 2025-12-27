package com.gdn.x.mta.distributiontask.rest.model.response;

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
public class BulkVendorProductActionsResponse extends BaseDTOResponse {
  private static final long serialVersionUID = -7473744004872365315L;

  List<VendorProductActionsResponse> vendorProductActionsResponses = new ArrayList<>();
}
