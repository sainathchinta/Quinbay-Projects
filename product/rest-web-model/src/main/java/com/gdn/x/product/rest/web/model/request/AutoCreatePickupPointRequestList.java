package com.gdn.x.product.rest.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoCreatePickupPointRequestList extends BaseRequest {
  private static final long serialVersionUID = 529146284094788846L;
  private List<AutoCreatePickupPointRequest> autoCreatePickupPointRequests = new ArrayList<>();
}
