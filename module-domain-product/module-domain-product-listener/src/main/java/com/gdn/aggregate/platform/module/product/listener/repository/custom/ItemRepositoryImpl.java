package com.gdn.aggregate.platform.module.product.listener.repository.custom;

import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

class ItemRepositoryCustomImpl implements ItemRepositoryCustom {

    public static final String PRODUCT_SKU = "productSku";
    public static final String ITEM_CODE = "itemCode";
    private final MongoTemplate mongoTemplate;

    public ItemRepositoryCustomImpl(@Qualifier("mainMongoTemplate") MongoTemplate mongoTemplate) {
        this.mongoTemplate = mongoTemplate;
    }

    @Override
    public Set<String> findItemCodesAndDeleteByProductSku(String productSku) {
        // Fetch itemCodes and delete items by productSku
        Query query = new Query(Criteria.where(PRODUCT_SKU).is(productSku));
        query.fields().include(ITEM_CODE);
        List<Item> items = mongoTemplate.find(query, Item.class);
        Set<String> itemCodes = items.stream().map(Item::getItemCode).filter(Objects::nonNull)
          .collect(Collectors.toSet());
        mongoTemplate.remove(query, Item.class);
        return itemCodes;
    }
}
