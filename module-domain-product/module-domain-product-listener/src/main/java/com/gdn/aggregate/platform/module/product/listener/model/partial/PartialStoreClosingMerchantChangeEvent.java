package com.gdn.aggregate.platform.module.product.listener.model.partial;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomMerchant;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class PartialStoreClosingMerchantChangeEvent extends BaseData {

    private CustomMerchant merchant;

}
