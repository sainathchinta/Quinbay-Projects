package com.gdn.x.product.model.vo;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class HalalDashboardFilterRequestVo {
  private String keyword;
  private List<Integer> curationStatus = new ArrayList<>();
  private List<String> categories = new ArrayList<>();
  private List<String> brands = new ArrayList<>();
}
