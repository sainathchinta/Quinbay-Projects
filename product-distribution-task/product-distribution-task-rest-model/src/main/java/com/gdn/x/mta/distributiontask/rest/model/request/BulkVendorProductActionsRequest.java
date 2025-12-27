package com.gdn.x.mta.distributiontask.rest.model.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkVendorProductActionsRequest implements Serializable {

  private static final long serialVersionUID = -8755893232540786232L;

  private String storeId;
  private String requestId;
  private String userName;
  private String actionType;
  private String assignmentType;
  private List<BulkScreeningProductActionsRequest> bulkScreeningProductActionsRequests;

}
