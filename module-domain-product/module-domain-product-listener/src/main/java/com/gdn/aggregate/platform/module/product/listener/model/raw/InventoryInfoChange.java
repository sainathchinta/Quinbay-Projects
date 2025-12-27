package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class InventoryInfoChange extends BaseData {

    private String itemSku;

    private String pickupPointCode;

    @Override
    public List<String> toIds() {
        String id = MainUtil.toNotNullString(this.itemSku);
        String subId = MainUtil.toNotNullString(this.pickupPointCode);
        return MainUtil.toList(id,subId);
    }

}
