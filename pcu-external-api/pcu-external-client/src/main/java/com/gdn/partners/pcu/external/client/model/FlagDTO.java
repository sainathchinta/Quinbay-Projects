package com.gdn.partners.pcu.external.client.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FlagDTO implements Serializable {
   private static final long serialVersionUID = -8669620235611199832L;
   private boolean deliveryFlag;
   private Boolean warehouseDistribution;
}
