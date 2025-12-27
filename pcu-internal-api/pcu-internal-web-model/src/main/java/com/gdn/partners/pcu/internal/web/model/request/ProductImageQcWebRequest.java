package com.gdn.partners.pcu.internal.web.model.request;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductImageQcWebRequest {

  private String productCode;
  private List<ImageFeedbackWebRequest> imageFeedback;
  private OtherModelFeedBack otherModelFeedBack;

}
