package com.gdn.partners.pcu.external.web.model.response;

import java.io.Serializable;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
public class ProductSystemParameterSwitchWebResponse implements Serializable {

  private static final long serialVersionUID = 995724854342771185L;

  Map<String, Object> productSystemParameterSwitchValues;
}
