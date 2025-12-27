package com.gdn.partners.pcu.internal.web.model.response;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductModelFeedback {
  private Set<String> systemFeedback = new HashSet<>();
  private Set<String> userFeedback = new HashSet<>();
}
