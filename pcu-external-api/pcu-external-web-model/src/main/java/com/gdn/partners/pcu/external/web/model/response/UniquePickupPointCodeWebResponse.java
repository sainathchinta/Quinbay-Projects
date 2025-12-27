package com.gdn.partners.pcu.external.web.model.response;

import java.util.HashSet;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class UniquePickupPointCodeWebResponse extends BaseResponse {
  private Set<String> itemSkus;
  private Set<String> pickupPointCodes;
}
