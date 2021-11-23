package cn.lili.common.sensitive;

import java.io.Serializable;
import java.util.TreeSet;

/**
 * @Description: 敏感词节点，每个节点包含了以相同的2个字符开头的所有词
 * @author Bulbasaur
 * @version v1.0
 * @since v1.0
 * 2020-02-25 14:10:16
 */
public class SensitiveWordsNode implements Serializable{

    /**
     * 头两个字符的mix，mix相同，两个字符相同
     */
    protected final int headTwoCharMix;

    /**
     * 所有以这两个字符开头的词表
     */
    protected final TreeSet<StringPointer> words = new TreeSet<StringPointer>();

    /**
     * 下一个节点
     */
    protected SensitiveWordsNode next;

    public SensitiveWordsNode(int headTwoCharMix){
        this.headTwoCharMix = headTwoCharMix;
    }

    public SensitiveWordsNode(int headTwoCharMix, SensitiveWordsNode parent){
        this.headTwoCharMix = headTwoCharMix;
        parent.next = this;
    }
}
