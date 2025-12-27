package com.gdn.partners.pcu.internal.client.model.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class BPJPHData implements Serializable {

  private static final long serialVersionUID = 8281592955227088742L;

  private Long total_items;
  private int total_pages;
  private int current_page;
  private List<HalalCertificationDetailResponse> datas = new ArrayList<>();
}
