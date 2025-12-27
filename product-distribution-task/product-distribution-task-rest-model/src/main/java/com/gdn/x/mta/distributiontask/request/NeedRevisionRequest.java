package com.gdn.x.mta.distributiontask.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.mta.distributiontask.rest.model.request.ItemNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductNotesRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class NeedRevisionRequest implements Serializable {

  private static final long serialVersionUID = -3569811481660572348L;
  private String storeId;
  private List<String> productCodes = new ArrayList();
  private String assignTo;
  private String assignedBy;
  private String correctionReason;
  private String additionalNotes;
  private String rejectionReason;
  private String needRevisionType;
  private ProductNotesRequest productNotesRequest;
  private List<ItemNotesRequest> itemNotes;
}
