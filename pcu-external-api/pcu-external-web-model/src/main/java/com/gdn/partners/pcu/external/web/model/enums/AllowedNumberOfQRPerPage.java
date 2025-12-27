package com.gdn.partners.pcu.external.web.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum AllowedNumberOfQRPerPage {
  ONE(1),
  TWO(2),
  FOUR(4),
  SIXTEEN(16),
  EIGHTEEN(18),
  THIRTY_TWO(32);

  private int numberValue;
}
