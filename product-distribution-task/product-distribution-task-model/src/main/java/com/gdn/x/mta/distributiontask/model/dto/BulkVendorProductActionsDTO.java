package com.gdn.x.mta.distributiontask.model.dto;

import java.io.Serializable;
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
public class BulkVendorProductActionsDTO implements Serializable {

  private static final long serialVersionUID = -9145745653770355506L;

  private List<BulkScreeningProductActionsDTO> bulkScreeningProductActionsRequests;
}
