package com.gdn.aggregate.platform.module.product.listener.constants;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;

import java.util.List;

public interface CampaignType {

  String FLASH_SALE = "FLASH_SALE";
  String SPECIAL = "SPECIAL";
  String REGULAR = "REGULAR";
  String NON_CAMPAIGN = "NON_CAMPAIGN";
  List<String> CAMPAIGNS = MainUtil.toList(FLASH_SALE,REGULAR,SPECIAL);

}
