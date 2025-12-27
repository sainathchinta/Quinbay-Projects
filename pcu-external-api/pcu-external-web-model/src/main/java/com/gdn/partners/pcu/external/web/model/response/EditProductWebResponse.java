package com.gdn.partners.pcu.external.web.model.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
public class EditProductWebResponse implements Serializable {

  private static final long serialVersionUID = -8881186153584286561L;
  private boolean productReview;
  private String reviewType;
  private String apiErrorCode;
  private String errorMessage;
  private List<VariantsErrorListWebResponse> variantsErrorList = new ArrayList();
}
