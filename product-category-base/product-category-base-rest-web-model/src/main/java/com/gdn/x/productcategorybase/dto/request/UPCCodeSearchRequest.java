package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class UPCCodeSearchRequest implements Serializable {

  private static final long serialVersionUID = 2765676922246927139L;
  List<String> categoryCodes;
  List<String> categoryIds;
  String upcCode;
}
