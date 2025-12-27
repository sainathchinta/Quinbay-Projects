package com.gda.mta.product.dto;

import java.io.Serial;
import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PCBAddEditVideoRequest implements Serializable {
  @Serial
  private static final long serialVersionUID = -6379953365543975779L;
  private String videoUrl;
  private String videoId;
  private String videoName;
  private String coverImagePath;
}
