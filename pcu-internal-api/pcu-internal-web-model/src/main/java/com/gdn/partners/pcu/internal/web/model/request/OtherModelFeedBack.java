package com.gdn.partners.pcu.internal.web.model.request;

import java.util.HashSet;
import java.util.Set;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class OtherModelFeedBack {
  private Set<String> userFeedback = new HashSet<>();
}
