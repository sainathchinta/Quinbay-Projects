package com.gdn.partners.pcu.internal.client.model.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class Margin implements Serializable {
  private static final long serialVersionUID = 4950178453017067950L;
  private String marginId;
  private String submissionId;
  private String submissionGroupId;
  private String replacementType;
  private String marginType;
  private String marginName;
  private Double marginPercentage;
  private Double minimumMargin;
  private Double maximumMargin;
  private Double transactionFee;
  private String feeType;
  private String calculationType;
  private VolumeMetricDTO volumeMetric;
  private List<MarginFactor> factors = new ArrayList<>();
  private List<Margin> replacedMargin;
}