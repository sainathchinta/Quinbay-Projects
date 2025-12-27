package com.gda.mta.product.dto.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageQcResponse implements Serializable {

  private static final long serialVersionUID = 5652830867742713987L;
  private String locationPath;
  private String hashCode;
  private List<ImageQcPredictionResponse> predictions;
  private boolean edited;
  private boolean markForDelete;
}
