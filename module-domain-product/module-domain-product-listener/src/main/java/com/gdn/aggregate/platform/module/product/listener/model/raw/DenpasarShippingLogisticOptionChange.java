package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.fasterxml.jackson.annotation.JsonProperty;
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
public class DenpasarShippingLogisticOptionChange extends BaseData {

    @JsonProperty("productId")
    private String itemSku;

    @JsonProperty("logisticOptionCode")
    private List<String> logisticOptions;

    @Override
    public List<String> toIds() {
        String id = MainUtil.toNotNullString(this.itemSku);
        return MainUtil.toList(id);
    }

}
