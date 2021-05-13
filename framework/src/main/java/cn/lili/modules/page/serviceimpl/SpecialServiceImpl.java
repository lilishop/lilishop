package cn.lili.modules.page.serviceimpl;

import cn.lili.modules.page.entity.dos.PageData;
import cn.lili.modules.page.entity.dos.Special;
import cn.lili.modules.page.mapper.SpecialMapper;
import cn.lili.modules.page.service.PageDataService;
import cn.lili.modules.page.service.SpecialService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 专题活动业务层实现
 *
 * @author Bulbasaur
 * @date 2020/12/7 11:27
 */
@Service
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class SpecialServiceImpl extends ServiceImpl<SpecialMapper, Special> implements SpecialService {

    //专题
    private final SpecialMapper specialMapper;
    //页面数据
    private final PageDataService pageDataService;

    @Override
    public Special addSpecial(Special special) {
        //新建页面
        PageData pageData=new PageData();
        pageDataService.save(pageData);

        //设置专题页面
        special.setPageDataId(pageData.getId());
        this.save(special);
        return special;
    }

    @Override
    public boolean removeSpecial(String id) {

        //删除页面内容
        Special special=this.getById(id);
        pageDataService.removeById(special.getPageDataId());

        //删除专题
        return this.removeById(id);
    }
}
