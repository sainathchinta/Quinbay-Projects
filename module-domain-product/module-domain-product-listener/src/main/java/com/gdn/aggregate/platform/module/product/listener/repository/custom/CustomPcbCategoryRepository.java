package com.gdn.aggregate.platform.module.product.listener.repository.custom;

import java.util.List;
import java.util.Set;

import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomPcbCategory;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Component;

@Component("ProductCustomPcbRepository")
public interface CustomPcbCategoryRepository extends MongoRepository<CustomPcbCategory, String> {

  CustomPcbCategory findByCategoryCode(String categoryCode);

  List<CustomPcbCategory> findByCategoryCodeIn(Set<String> categoryCodes);
}
