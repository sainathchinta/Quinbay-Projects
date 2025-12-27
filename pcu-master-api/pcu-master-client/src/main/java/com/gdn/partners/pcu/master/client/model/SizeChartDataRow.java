package com.gdn.partners.pcu.master.client.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SizeChartDataRow {
  private int rowNumber;
  private List<SizeChartDataColumn> columns = new ArrayList<>();
}
