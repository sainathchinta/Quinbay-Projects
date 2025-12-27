package com.gdn.x.product.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AddDeleteVariantRequestVo {
  List<AddVariantRequestVo> addVariantsList = new ArrayList<>();
  List<String> deleteVariantsList = new ArrayList<>();
}