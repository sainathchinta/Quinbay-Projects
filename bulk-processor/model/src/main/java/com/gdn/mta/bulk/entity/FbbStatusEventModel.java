package com.gdn.mta.bulk.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.dto.FbbFailedItems;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class FbbStatusEventModel {

  private String consignmentId;
  private List<FbbFailedItems> failedItems;
  private String result;
}
