package com.gdn.x.mta.distributiontask.rest.model.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class AutoNeedRevisionRequest implements Serializable {

  private static final long serialVersionUID = -4009474322534305002L;

  private String productCode;
  private String state;
  private String reason;
  private boolean edited;
  private String editedState;
  private String imageReason;
  private String contentReason;
  private List<String> vendorErrorFields = new ArrayList<>();

  public AutoNeedRevisionRequest(String productCode, String state, String reason, boolean edited, String editedState,
      String imageReason) {
    this.productCode = productCode;
    this.state = state;
    this.reason = reason;
    this.edited = edited;
    this.editedState = editedState;
    this.imageReason = imageReason;
  }
}
