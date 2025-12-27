package com.gdn.partners.pcu.external.client.helper;

import lombok.Builder;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
@Builder
public class ReelsResponse<T> {
  private String status;
  private T data;
  private Map<String, List<String>> errors;
  private Map<String, Object> metadata;
  private Paging paging;
}
