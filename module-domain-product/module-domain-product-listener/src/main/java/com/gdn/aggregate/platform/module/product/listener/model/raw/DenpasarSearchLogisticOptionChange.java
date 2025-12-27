package com.gdn.aggregate.platform.module.product.listener.model.raw;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;
import java.util.Map;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class DenpasarSearchLogisticOptionChange extends BaseData {

    private String itemSku;

    @JsonProperty("sku")
    private String productSku;

    private List<String> tags;

    private Map<String, String> badge;

}
