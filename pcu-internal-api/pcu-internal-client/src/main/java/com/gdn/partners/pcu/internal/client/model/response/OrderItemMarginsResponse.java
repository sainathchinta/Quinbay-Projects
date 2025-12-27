package com.gdn.partners.pcu.internal.client.model.response;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.partners.pcu.internal.client.model.request.Margin;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OrderItemMarginsResponse implements Serializable {

  private static final long serialVersionUID = 2941355706433862697L;
  private String orderItemId;
  private List<Margin> margins = new ArrayList<>();
}