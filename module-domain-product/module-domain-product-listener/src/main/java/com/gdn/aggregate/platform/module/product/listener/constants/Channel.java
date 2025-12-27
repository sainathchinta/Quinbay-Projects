package com.gdn.aggregate.platform.module.product.listener.constants;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;

import java.util.List;

public interface Channel {

  String DEFAULT = "DEFAULT";
  String CNC = "CNC";
  List<String> NECESSARY_CHANNELS = MainUtil.toList(DEFAULT,CNC);

}
