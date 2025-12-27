package com.gdn.aggregate.platform.module.product.listener.repository.semi;

import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.List;
import java.util.Set;

public interface SivaFlashsaleGroupRepository extends MongoRepository<SivaFlashsaleGroup, String> {

  List<SivaFlashsaleGroup> findAllByIdIn(Set<String> ids);

}
