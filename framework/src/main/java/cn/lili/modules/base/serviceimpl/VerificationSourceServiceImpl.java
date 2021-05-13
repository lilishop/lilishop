package cn.lili.modules.base.serviceimpl;

import cn.lili.common.cache.Cache;
import cn.lili.modules.base.entity.dos.VerificationSource;
import cn.lili.modules.base.entity.enums.VerificationSourceEnum;
import cn.lili.modules.base.entity.vo.VerificationVO;
import cn.lili.modules.base.mapper.VerificationSourceMapper;
import cn.lili.modules.base.service.VerificationSourceService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 验证码资源维护 业务层实现
 *
 * @author Chopper
 * @date 2020/11/17 3:48 下午
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class VerificationSourceServiceImpl extends ServiceImpl<VerificationSourceMapper, VerificationSource> implements VerificationSourceService {

    @Autowired
    private Cache<VerificationVO> cache;

    @Override
    public VerificationVO initCache() {
        List<VerificationSource> dbList = this.list();
        List<VerificationSource> resourceList = new ArrayList<>();
        List<VerificationSource> sliderList = new ArrayList<>();
        for (VerificationSource item : dbList) {
            if (item.getType().equals(VerificationSourceEnum.RESOURCE.name())) {
                resourceList.add(item);
            } else if (item.getType().equals(VerificationSourceEnum.SLIDER.name())) {
                sliderList.add(item);
            }
        }
        VerificationVO verificationVO = new VerificationVO();
        verificationVO.setVerificationResources(resourceList);
        verificationVO.setVerificationSlider(sliderList);
        cache.put(VERIFICATION_CACHE, verificationVO);
        return verificationVO;
    }

    @Override
    public VerificationVO getVerificationCache() {
        VerificationVO verificationVO = cache.get(VERIFICATION_CACHE);
        if (verificationVO == null) {
            return initCache();
        }
        return verificationVO;
    }
}